
// region: error handling
use derive_more::From;

#[derive(Debug, From)]
pub enum AutomatonError<OtherError: std::fmt::Debug>
{
    // #[error("can't update a machine if the machine is finished")]
    Finished,

    //#[error("{}", other_err)]
    #[from]
    Other(OtherError),
}

impl <OtherError: std::fmt::Debug> std::fmt::Display for AutomatonError<OtherError> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl <OtherError: std::fmt::Debug> std::error::Error for AutomatonError<OtherError> {}


// endregion

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum RunInfo {
    Ready,
    Running,
    Finished,
}

pub trait Automaton<INPUT, OUTPUT, ERROR: std::fmt::Debug>
{
    // INPUT is what the machine reads
    fn clear(&mut self);
    fn get_run_info(&self) -> RunInfo;
    fn update(&mut self, input: INPUT) -> std::result::Result<(), AutomatonError<ERROR>>;
    // if the machine is finished, get_state returns a reference to the last state the machine was in
    fn get_state(&self) -> OUTPUT;
    fn finish(&mut self);
 
    fn is_finished(&self) -> bool {
        self.get_run_info() == RunInfo::Finished
    }
    fn is_ready(&self) -> bool {
        self.get_run_info() == RunInfo::Ready
    }
    fn is_running(&self) -> bool {
        self.get_run_info() == RunInfo::Running
    }
}