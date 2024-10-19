using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.PatientLoggers;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.PatientLoggers
{
    public class PatientLoggerRepository : BaseRepository<PatientLogger, PatientLoggerId>, IPatientLoggerRepository
    {
        private readonly DDDSample1DbContext context;

        public PatientLoggerRepository(DDDSample1DbContext context) : base(context.PatientLoggers)
        {
            this.context = context;
        }

        public Task<List<PatientLogger>> GetOldLogsAsync(DateTime cutoffDate)
        {
            return context.PatientLoggers
                  .Where(logger => logger.ModificationDate <= cutoffDate)
                  .ToListAsync();
        }
    }
}