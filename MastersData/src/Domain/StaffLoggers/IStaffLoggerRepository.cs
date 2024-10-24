using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.StaffLoggers
{
    public interface IStaffLoggerRepository : IRepository<StaffLogger, StaffLoggerId>
    {
        Task<List<StaffLogger>> GetOldLogsAsync(DateTime cutoffDate);
    }
}