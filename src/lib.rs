use orange_trees::{Node as OrangeNode, Tree as OrangeTree};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::Style,
    widgets::{Block, StatefulWidget, Widget},
};
use std::{iter, vec::Vec};
use unicode_width::UnicodeWidthStr;

// Re-export the Node and Tree types from orange-trees
pub type Node<V> = OrangeNode<String, V>;
pub type Tree<V> = OrangeTree<String, V>;

/// Trait for node values that can be rendered in the tree
pub trait NodeValue: Default {
    /// Return iterator over render parts - text with its style
    /// If style is `None`, then it will use the widget's default style
    fn render_parts_iter(&self) -> impl Iterator<Item = (&str, Option<Style>)>;
}

impl NodeValue for String {
    fn render_parts_iter(&self) -> impl Iterator<Item = (&str, Option<Style>)> {
        iter::once((self.as_str(), None))
    }
}

/// Tracks the state of the tree widget (opened/closed nodes, selection)
#[derive(Default, Clone)]
pub struct TreeState {
    /// Tracks open nodes by their IDs
    open: Vec<String>,
    /// Currently selected node ID
    selected: Option<String>,
}

impl TreeState {
    pub fn selected(&self) -> Option<&str> {
        self.selected.as_deref()
    }

    pub fn select<V>(&mut self, tree: &Tree<V>, node: &Node<V>) {
        self.open_ancestors(tree, node);
        self.selected = Some(node.id().to_string());
    }

    pub fn is_open<V>(&self, node: &Node<V>) -> bool {
        self.open.contains(node.id())
    }

    pub fn is_selected<V>(&self, node: &Node<V>) -> bool {
        self.selected
            .as_ref()
            .map(|x| x == node.id())
            .unwrap_or(false)
    }

    pub fn open<V>(&mut self, _tree: &Tree<V>, node: &Node<V>) {
        if !node.is_leaf() && !self.is_open(node) {
            self.open.push(node.id().to_string());
            self.open_ancestors(_tree, node);
        }
    }

    pub fn close<V>(&mut self, _tree: &Tree<V>, node: &Node<V>) {
        self.open.retain(|x| x != node.id());
        self.close_children(node);
    }

    pub fn move_up<V>(&mut self, tree: &Tree<V>) {
        if let Some(selected) = self.selected.take() {
            if let Some(parent) = tree.root().parent(&selected) {
                self.selected = Some(
                    self.previous_sibling(tree, tree.root().query(&selected).unwrap())
                        .map(|x| self.get_last_open_heir(x))
                        .unwrap_or(parent)
                        .id()
                        .to_string(),
                );
            } else {
                self.selected = Some(selected);
            }
        }
    }

    pub fn move_down<V>(&mut self, tree: &Tree<V>) {
        if let Some(selected) = self.selected.take() {
            if let Some(node) = tree.root().query(&selected) {
                if !node.is_leaf() && self.is_open(node) {
                    self.selected = Some(node.iter().next().unwrap().id().to_string());
                } else if let Some(sibling) = self.next_sibling(tree, node) {
                    self.selected = Some(sibling.id().to_string());
                } else {
                    let mut current = selected.clone();
                    loop {
                        if let Some(parent) = tree.root().parent(&current) {
                            current = parent.id().to_string();
                            if let Some(sibling) = self.next_sibling(tree, parent) {
                                self.selected = Some(sibling.id().to_string());
                                break;
                            }
                        } else {
                            self.selected = Some(selected);
                            break;
                        }
                    }
                }
            }
        }
    }

    fn close_children<V>(&mut self, node: &Node<V>) {
        node.iter().for_each(|x| {
            self.open.retain(|id| id != x.id());
            self.close_children(x);
        });
    }

    fn open_ancestors<V>(&mut self, tree: &Tree<V>, node: &Node<V>) {
        if let Some(parent) = tree.root().parent(node.id()) {
            self.open.push(parent.id().to_string());
            self.open_ancestors(tree, parent);
        }
    }

    fn previous_sibling<'a, V>(&self, tree: &'a Tree<V>, node: &'a Node<V>) -> Option<&'a Node<V>> {
        let parent = tree.root().parent(node.id())?;
        let mut prev_node = None;
        for child in parent.iter() {
            if child.id() == node.id() {
                break;
            }
            prev_node = Some(child);
        }
        prev_node
    }

    fn next_sibling<'a, V>(&self, tree: &'a Tree<V>, node: &'a Node<V>) -> Option<&'a Node<V>> {
        let parent = tree.root().parent(node.id())?;
        let mut keep_next = false;
        for child in parent.iter() {
            if keep_next {
                return Some(child);
            } else if child.id() == node.id() {
                keep_next = true;
            }
        }
        None
    }

    fn get_last_open_heir<'a, V>(&self, node: &'a Node<V>) -> &'a Node<V> {
        if self.is_open(node) && !node.is_leaf() {
            self.get_last_open_heir(node.iter().last().unwrap())
        } else {
            node
        }
    }
}

pub struct TreeWidget<'a, V: NodeValue> {
    /// The tree data structure
    tree: &'a Tree<V>,
    /// Optional block to wrap the widget
    block: Option<Block<'a>>,
    /// Base style for the widget
    style: Style,
    /// Style for highlighted (selected) items
    highlight_style: Style,
    /// Optional symbol to show before highlighted items
    highlight_symbol: Option<String>,
    /// Number of spaces for each level of indentation
    indent_size: usize,
}

impl<'a, V: NodeValue> TreeWidget<'a, V> {
    pub fn new(tree: &'a Tree<V>) -> Self {
        Self {
            tree,
            block: None,
            style: Style::default(),
            highlight_style: Style::default(),
            highlight_symbol: None,
            indent_size: 4,
        }
    }

    pub fn block(mut self, block: Block<'a>) -> Self {
        self.block = Some(block);
        self
    }

    pub fn style(mut self, style: Style) -> Self {
        self.style = style;
        self
    }

    pub fn highlight_style(mut self, style: Style) -> Self {
        self.highlight_style = style;
        self
    }

    pub fn highlight_symbol(mut self, symbol: String) -> Self {
        self.highlight_symbol = Some(symbol);
        self
    }

    pub fn indent_size(mut self, size: usize) -> Self {
        self.indent_size = size;
        self
    }

    fn render_node(
        &self,
        node: &Node<V>,
        area: Rect,
        buf: &mut Buffer,
        state: &TreeState,
        depth: usize,
    ) -> Rect {
        let highlight_symbol = if state.is_selected(node) {
            self.highlight_symbol.clone()
        } else {
            None
        };

        let node_area = Rect {
            x: area.x,
            y: area.y,
            width: area.width,
            height: 1,
        };

        let style = if state.is_selected(node) {
            self.highlight_style
        } else {
            self.style
        };

        buf.set_style(node_area, style);

        let mut indent_size = depth * self.indent_size;
        if state.is_selected(node) && highlight_symbol.is_some() {
            indent_size =
                indent_size.saturating_sub(highlight_symbol.as_ref().unwrap().width() + 1);
        }

        let width = area.width as usize;
        let mut start_x = area.x;
        let start_y = area.y;

        // Write indentation
        if indent_size > 0 {
            buf.set_stringn(start_x, start_y, " ".repeat(indent_size), width, style);
            start_x += indent_size as u16;
        }

        // Write highlight symbol
        if let Some(symbol) = highlight_symbol {
            buf.set_stringn(start_x, start_y, &symbol, width - start_x as usize, style);
            start_x += (symbol.width() + 1) as u16;
        }

        // Write node content
        for (text, part_style) in node.value().render_parts_iter() {
            let text_style = part_style.unwrap_or(style);
            buf.set_stringn(start_x, start_y, text, width - start_x as usize, text_style);
            start_x += text.width() as u16;
        }

        // Write arrow indicator
        let arrow = if state.is_open(node) {
            " ▼" // Arrow down
        } else if node.is_leaf() {
            " " // No arrow for leaves
        } else {
            " ▶" // Arrow right
        };
        buf.set_stringn(start_x, start_y, arrow, width - start_x as usize, style);

        // Return the next available area
        Rect {
            x: area.x,
            y: area.y + 1,
            width: area.width,
            height: area.height.saturating_sub(1),
        }
    }

    fn render_tree(
        &self,
        node: &Node<V>,
        mut area: Rect,
        buf: &mut Buffer,
        state: &TreeState,
        depth: usize,
    ) -> Rect {
        // Render current node
        area = self.render_node(node, area, buf, state, depth);

        // If node is open, render children
        if state.is_open(node) && area.height > 0 {
            for child in node.iter() {
                if area.height == 0 {
                    break;
                }
                area = self.render_tree(child, area, buf, state, depth + 1);
            }
        }

        area
    }
}

impl<'a, V: NodeValue> Widget for TreeWidget<'a, V> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let mut state = TreeState::default();
        StatefulWidget::render(self, area, buf, &mut state);
    }
}

impl<'a, V: NodeValue> StatefulWidget for TreeWidget<'a, V> {
    type State = TreeState;

    fn render(mut self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        let area = match self.block.take() {
            Some(b) => {
                let inner = b.inner(area);
                b.render(area, buf);
                inner
            }
            None => area,
        };

        if area.width < 1 || area.height < 1 {
            return;
        }

        self.render_tree(self.tree.root(), area, buf, state, 0);
    }
}
