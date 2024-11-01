using DDDSample1.Domain.Patients;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Patients
{
    public class MedicalHistoryRepository : BaseRepository<MedicalHistory, MedicalHistoryId>, IMedicalHistoryRepository
    {
        private readonly DDDSample1DbContext context;
        public MedicalHistoryRepository(DDDSample1DbContext context) : base(context.MedicalHistories)
        {
            this.context = context;
        }
    }
}