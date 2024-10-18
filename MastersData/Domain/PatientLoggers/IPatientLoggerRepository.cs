using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.PatientLoggers
{
    public interface IPatientLoggerRepository : IRepository<PatientLogger, PatientLoggerId>
    {
        Task<List<PatientLogger>> GetOldLogsAsync(DateTime cutoffDate);
    }
}