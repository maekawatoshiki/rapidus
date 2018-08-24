pub type Id = usize;

#[derive(Clone, Debug, PartialEq)]
pub struct IdGen {
    pub id: Vec<Id>,
}

impl IdGen {
    pub fn new() -> IdGen {
        IdGen { id: vec![0] }
    }

    pub fn gen_id(&mut self) -> Id {
        let id = *self.id.last().unwrap();
        *self.id.last_mut().unwrap() += 1;
        id
    }

    pub fn get_cur_id(&mut self) -> Id {
        *self.id.last().unwrap()
    }

    pub fn save(&mut self) {
        self.id.push(0)
    }

    pub fn restore(&mut self) {
        self.id.pop();
    }
}

#[test]
fn test() {
    let mut idgen = IdGen::new();
    assert_eq!(idgen.gen_id(), 0);
    assert_eq!(idgen.gen_id(), 1);
    assert_eq!(idgen.get_cur_id(), 2);
    idgen.save();
    assert_eq!(idgen.gen_id(), 0);
    assert_eq!(idgen.gen_id(), 1);
    assert_eq!(idgen.get_cur_id(), 2);
    idgen.restore();
    assert_eq!(idgen.gen_id(), 2);
    assert_eq!(idgen.gen_id(), 3);
    assert_eq!(idgen.get_cur_id(), 4);
}
