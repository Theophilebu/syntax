use std::error::Error;

use thiserror::Error;

#[derive(PartialEq, Eq)]
pub enum RunInfo {
    Ready,
    Running,
    Finished,
}

#[derive(Error, Debug)]
pub enum MachineError<OTHER_ERROR: Error>
where
    MachineError<OTHER_ERROR>: From<OTHER_ERROR>
{
    #[error("can't update a machine if the machine is finished")]
    Finished,

    #[error("{}", other_err)]
    Other {other_err: OTHER_ERROR},
}

pub trait Machine<INPUT, OUTPUT, ERROR: Error>
where
    MachineError<ERROR>: From<ERROR>
{
    // SYMBOL is what the machine reads
    // STATE is the machine's internal state that it shows
    fn clear(&mut self);
    fn get_run_info(& self) -> &RunInfo;
    fn update(&mut self, symbol: &INPUT) -> Result<(), MachineError<ERROR>>;
    fn get_state(&self) -> &OUTPUT;
    // if the machine is finished, get_state returns a reference to the last state the machine was in
 
    fn is_finished(&self) -> bool {
        *self.get_run_info() == RunInfo::Finished
    }
    fn is_ready(&self) -> bool {
        *self.get_run_info() == RunInfo::Ready
    }
    fn is_running(&self) -> bool {
        *self.get_run_info() == RunInfo::Running
    }
}


pub trait UnendingMachine<INPUT, OUTPUT, ERROR: Error>: Machine<INPUT, OUTPUT, ERROR>
where
    MachineError<ERROR>: From<ERROR>
{
    fn finish(&mut self);
}