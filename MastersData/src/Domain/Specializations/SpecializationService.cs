using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using DDDSample1.Infrastructure;

namespace DDDSample1.Domain.Specializations
{
    public class SpecializationService 
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly ISpecializationRepository _repo;

        public SpecializationService(IUnitOfWork unitOfWork, ISpecializationRepository SpecializationRepository)
        {
            _unitOfWork = unitOfWork;
            _repo = SpecializationRepository;
        }
        

        public async Task<Specialization> GetByIdAsync(SpecializationId id)
        {   
    
            var op = await this._repo.GetByIdAsync(id);
            if (op == null)
                return null;

            return op;

        }
    }
}
