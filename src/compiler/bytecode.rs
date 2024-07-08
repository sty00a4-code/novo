#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct ByteCodeLocated {
    pub code: ByteCode,
    pub ln: u32
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
#[repr(u8)]
pub enum ByteCode {
    #[default]
    None,
    Jump {
        addr: Address,
    },
    JumpIf {
        not: bool,
        addr: Address,
        src: Source
    },
    
    Call {
        func: Source,
        start: Register,
        amount: u8,
        dst: Option<Register>
    },
    Return { src: Option<Source> },

    Move { dst: Register, src: Source },
    Field {
        dst: Register,
        head: Source,
        field: Source,
    },
    SetField {
        head: Source,
        field: Source,
        src: Source,
    },

    Binary {
        op: BinaryOperation,
        dst: Register,
        left: Source,
        right: Source,
    },
    Unary {
        op: UnaryOperation,
        dst: Register,
        right: Source,
    },
}
pub type Register = u16;
pub type Address = u16;
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Source {
    #[default]
    None,
    Register(Register),
    String(Address),
    Int(Address),
    Float(Address),
    Bool(bool),
    Char(char),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum BinaryOperation {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    Pow,
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,
    And,
    Or,
}
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOperation {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Closure {
    pub code: Vec<ByteCodeLocated>,
    pub registers: Register,
    pub strings: Vec<String>,
    pub ints: Vec<i64>,
    pub floats: Vec<f64>,
    pub path: Option<Box<str>>,
    pub upvalues: Vec<Upvalue>,
}
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Upvalue {
    register: Register,
    depth: u8,
}