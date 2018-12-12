// use chrono::Utc;
use super::{value::Value};
use id;
use std::collections::VecDeque;
use std::sync::mpsc;

pub type TimerID = id::Id;

pub struct TaskManager {
    id: id::IdGen,
    tasks: VecDeque<Task>,
    mirror_tasks: VecDeque<Task>,
}

#[derive(Debug, Clone)]
pub enum TimerKind {
    Timeout { now: i64, timeout: i64 },
    Interval { previous: i64, interval: i64 },
}

pub enum IoKind {
    Read { receiver: mpsc::Receiver<String> },
    Write,
}

pub enum Task {
    Timer {
        kind: TimerKind,
        id: TimerID,
        callback: Value,
        args: Vec<Value>,
    },
    Io {
        kind: IoKind,
        callback: Value,
    }, // TODO: Add I/O, microtasks...
}

impl Task {
    pub fn get_timer_id(&self) -> Option<TimerID> {
        match self {
            Task::Timer { id, .. } => Some(*id),
            Task::Io { .. } => None,
        }
    }

    pub fn get_timer_id_mut(&mut self) -> Option<&mut TimerID> {
        match self {
            Task::Timer { ref mut id, .. } => Some(id),
            Task::Io { .. } => None,
        }
    }
}

impl TaskManager {
    pub fn new() -> Self {
        TaskManager {
            id: id::IdGen::new(),
            tasks: VecDeque::new(),
            mirror_tasks: VecDeque::new(),
        }
    }

    pub fn add_io(&mut self, task: Task) {
        self.tasks.push_back(task)
    }

    pub fn add_timer(&mut self, mut task: Task) -> TimerID {
        let id = self.id.gen_id();
        *task.get_timer_id_mut().unwrap() = id;
        self.tasks.push_back(task);
        id
    }

    pub fn clear_timer(&mut self, id: TimerID) {
        if let Some(idx) = self
            .tasks
            .iter()
            .position(|task| task.get_timer_id() == Some(id))
        {
            self.tasks.remove(idx);
        }

        if let Some(idx) = self
            .mirror_tasks
            .iter()
            .position(|task| task.get_timer_id() == Some(id))
        {
            self.mirror_tasks.remove(idx);
        }
    }

    pub fn get_task(&mut self) -> Option<Task> {
        match self.tasks.pop_front() {
            Some(task) => Some(task),
            None => {
                self.tasks.append(&mut self.mirror_tasks);
                None
            }
        }
    }

    pub fn retain_task(&mut self, task: Task) {
        self.mirror_tasks.push_back(task)
    }

    pub fn no_tasks(&mut self) -> bool {
        self.tasks.len() == 0
    }
}
