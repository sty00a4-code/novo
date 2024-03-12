use std::{
    fmt::{Debug, Display},
    ops::Range,
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Position {
    pub ln: Range<usize>,
    pub col: Range<usize>,
}
pub struct Spanned<T> {
    pub value: T,
    pub span: Range<usize>,
}
pub struct Located<T> {
    pub value: T,
    pub pos: Position,
}
pub struct PathLocated<T> {
    pub value: T,
    pub pos: Position,
    pub path: Rc<str>,
}

impl Position {
    pub fn new(ln: Range<usize>, col: Range<usize>) -> Self {
        Self { ln, col }
    }
    pub fn single(ln: usize, col: usize) -> Self {
        Self {
            ln: ln..ln + 1,
            col: col..col + 1,
        }
    }
    pub fn extend(&mut self, other: &Self) {
        self.ln.end = other.ln.end;
        self.col.end = other.col.end;
    }
}
impl<T> Spanned<T> {
    pub fn new(value: T, span: Range<usize>) -> Self {
        Self { value, span }
    }
    pub fn map<U, F: Fn(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }
}
impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}
impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}
impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            span: self.span.clone(),
        }
    }
}
impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl<T: Default> Default for Spanned<T> {
    fn default() -> Self {
        Self {
            value: T::default(),
            span: Range::default(),
        }
    }
}
impl<T> Located<T> {
    pub fn new(value: T, pos: Position) -> Self {
        Self { value, pos }
    }
    pub fn map<U, F: Fn(T) -> U>(self, f: F) -> Located<U> {
        Located {
            value: f(self.value),
            pos: self.pos,
        }
    }
}
impl<T: Debug> Debug for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}
impl<T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}
impl<T: Clone> Clone for Located<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            pos: self.pos.clone(),
        }
    }
}
impl<T: PartialEq> PartialEq for Located<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl<T: Default> Default for Located<T> {
    fn default() -> Self {
        Self {
            value: T::default(),
            pos: Position::default(),
        }
    }
}
impl<T> PathLocated<T> {
    pub fn new(value: T, pos: Position, path: Rc<str>) -> Self {
        Self { value, pos, path }
    }
    pub fn map<U, F: Fn(T) -> U>(self, f: F) -> PathLocated<U> {
        PathLocated {
            value: f(self.value),
            pos: self.pos,
            path: self.path
        }
    }
}
impl<T: Debug> Debug for PathLocated<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}
impl<T: Display> Display for PathLocated<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}
impl<T: Clone> Clone for PathLocated<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            pos: self.pos.clone(),
            path: Rc::clone(&self.path)
        }
    }
}
impl<T: PartialEq> PartialEq for PathLocated<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
