using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Application.Dtos;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using DDDSample1.Infrastructure;

namespace DDDSample1.Domain.Specializations
{
    public class SpecializationService : ISpecializationService
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
                throw new Exception($"No specialization found with the name '{name}'.");

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

        public async Task<Dictionary<string, Guid>> GetSpecializationMapAsync()
        {
            var specializations = await _repo.GetSpecializationMapAsync();
            return specializations.ToDictionary(s => s.Key, s => s.Value.AsGuid());
        }

        public async Task<SpecializationDto> CreateAsync(SpecializationDto dto)
        {
            var specialization = new Specialization(dto.Name);

            await _repo.AddAsync(specialization);
            await _unitOfWork.CommitAsync();

            return new SpecializationDto(specialization.Id.AsString(), specialization.Name);
        }


    }
}
