using DDDSample1.Domain.PatientLoggers;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.PatientLoggers
{
    public class PatientLoggerRepository : BaseRepository<PatientLogger, PatientLoggerId>, IPatientLoggerRepository
    {
        private readonly DDDSample1DbContext context;

        public PatientLoggerRepository(DDDSample1DbContext context):base(context.PatientLoggers)
        {
            this.context = context;
        }
    }
}