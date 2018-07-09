#[derive(Clone, Debug, PartialEq)]
pub struct IdGen {
    pub id: Vec<usize>,
}

impl IdGen {
    pub fn new() -> IdGen {
        IdGen { id: vec![0] }
    }

    pub fn gen_id(&mut self) -> usize {
        let id = *self.id.last().unwrap();
        *self.id.last_mut().unwrap() += 1;
        id
    }

    pub fn get_cur_id(&mut self) -> usize {
        *self.id.last().unwrap()
    }

    pub fn save(&mut self) {
        self.id.push(0)
    }

    pub fn restore(&mut self) {
        self.id.pop();
    }
}
