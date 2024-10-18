using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.PatientLoggers
{
    public interface IPatientLoggerRepository : IRepository<PatientLogger, PatientLoggerId>
    {
    }
}