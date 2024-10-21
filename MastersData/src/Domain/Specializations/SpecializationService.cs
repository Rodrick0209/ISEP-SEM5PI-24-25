using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationTypes;
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

        public async Task<Specialization> GetByNameAsync(string name)
        {   
    
            var op = await this._repo.GetByNameAsync(name);
            if (op == null)
                return null;

            return op;

        }

        public async Task<Dictionary<Guid, string>> GetByNameOperationTypeAsync(OperationType op)
        {
            var specializationIds = op.preparationPhase.requiredStaff
                .Concat(op.surgeryPhase.requiredStaff)
                .Concat(op.cleaningPhase.requiredStaff)
                .Select(staff => staff.specialization)
                .Distinct()
                .ToList();

            var specializationNames = new Dictionary<Guid, string>();
            foreach (var specId in specializationIds)
            {
                var spec = await GetByIdAsync(new SpecializationId(specId.Value));
                specializationNames[specId.AsGuid()] = spec.Name;
            }

            return specializationNames;
        }

    }
}
