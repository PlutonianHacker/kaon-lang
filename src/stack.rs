pub struct Stack<T> {
    stack: Vec<T>,
}

impl<T> Stack<T> {
    pub fn new() -> Self {
        Stack { stack: vec![] }
    }

    pub fn push(&mut self, data: T) {
        self.stack.push(data);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.stack.pop()
    }

    pub fn peek(&mut self) -> Option<&T> {
        self.stack.get(self.stack.len() - 1)
    }
}
