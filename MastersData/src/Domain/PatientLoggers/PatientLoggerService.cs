using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.PatientLoggers
{
    public class PatientLoggerService
    {
        private readonly IUnitOfWork unitOfWork;
        private readonly IPatientLoggerRepository _repo;

        public PatientLoggerService(IUnitOfWork unitOfWork, IPatientLoggerRepository repo)
        {
            this.unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<List<PatientLogger>> GetAllAsync()
        {
            return await this._repo.GetAllAsync();
        }
    }
}