using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.StaffLoggers;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.StaffLoggers
{
    public class StaffLoggerRepository : BaseRepository<StaffLogger, StaffLoggerId>, IStaffLoggerRepository
    {
        private readonly DDDSample1DbContext context;

        public StaffLoggerRepository(DDDSample1DbContext context) : base(context.StaffLoggers)
        {
            this.context = context;
        }

        public Task<List<StaffLogger>> GetOldLogsAsync(DateTime cutoffDate)
        {
            return context.StaffLoggers
                  .Where(logger => logger.ModificationDate <= cutoffDate)
                  .ToListAsync();
        }
    }
}