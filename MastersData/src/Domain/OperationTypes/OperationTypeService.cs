using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure;
using Microsoft.AspNetCore.JsonPatch.Internal;
using System.Collections.Generic;
using Microsoft.AspNetCore.Routing.Constraints;
using System.Linq;

namespace DDDSample1.Domain.OperationTypes
{
    public class OperationTypeService : IOperationTypeService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IOperationTypeRepository _repo;

        private readonly IPhasesRepository _phaseRepo;

        public OperationTypeService(IUnitOfWork unitOfWork, IOperationTypeRepository operationTypeRepository, IPhasesRepository phaseRepo)
        {
            _unitOfWork = unitOfWork;
            _repo = operationTypeRepository;
            _phaseRepo = phaseRepo;
        }


        public async Task<OperationType> CreateAsync(OperationType obj)
        {
            // Validate if the operation type name is unique
            bool nameIsUnique = await ValidateNameIsUnique(obj.name);
            if (!nameIsUnique)
            {
                throw new BusinessRuleValidationException("Operation Type name already exists");
            }

            // Create the OperationType entity
            await this._repo.AddAsync(obj);

            await this._unitOfWork.CommitAsync();

            return obj;

        }

        private async Task<bool> ValidateNameIsUnique(string name)
        {
            var existingOperationType = await _repo.GetByNameAsync(name);
            return existingOperationType == null;
        }

        public async Task<OperationType> GetByIdAsync(OperationTypeId id)
        {
            var op = await this._repo.GetByIdAsync(id);
            if (op == null)
                return null;

            return op;

        }


        public async Task<OperationType> Deactivate(OperationTypeId id)
        {

            var op = await this._repo.GetByIdAsync(id);
            if (op == null)
                return null;

            op.Deactivate();

            await this._unitOfWork.CommitAsync();

            return op;

        }

        public async Task<List<OperationType>> GetAllAsync()
        {
            return await this._repo.GetAllAsync();
        }

        public async Task<OperationType> UpdateAsync(OperationTypeDto dto)
        {

            var op = await this._repo.GetByIdAsync(new OperationTypeId(dto.Id));
            if (op == null)
                return null;

            op.changeName(dto.Name);




            var preparation = await _phaseRepo.GetByIdAsync(new PhasesId(dto.PreparationPhase.Id));
            preparation.ChangeDuration(dto.PreparationPhase.Duration);
            var preparationRequiredStaffEntities = dto.PreparationPhase.RequiredStaff
                .Select(rsDto => new RequiredStaff(int.Parse(rsDto.num), new Specializations.SpecializationId(Guid.Parse(rsDto.Specialization))))
                .ToList();

            // Change the required staff for preparation phase
            preparation.ChangeRequiredStaff(preparationRequiredStaffEntities);

            var surgery = await _phaseRepo.GetByIdAsync(new PhasesId(dto.SurgeryPhase.Id));
            surgery.ChangeDuration(dto.SurgeryPhase.Duration);
            var surgeryRequiredStaffEntities = dto.SurgeryPhase.RequiredStaff
                .Select(rsDto => new RequiredStaff(int.Parse(rsDto.num), new Specializations.SpecializationId(Guid.Parse(rsDto.Specialization))))
                .ToList();

            // Change the required staff for surgery phase
            surgery.ChangeRequiredStaff(surgeryRequiredStaffEntities);

            var cleaning = await _phaseRepo.GetByIdAsync(new PhasesId(dto.CleaningPhase.Id));
            cleaning.ChangeDuration(dto.CleaningPhase.Duration);
            var cleaningRequiredStaffEntities = dto.CleaningPhase.RequiredStaff
                .Select(rsDto => new RequiredStaff(int.Parse(rsDto.num), new Specializations.SpecializationId(Guid.Parse(rsDto.Specialization))))
                .ToList();

            // Change the required staff for cleaning phase
            cleaning.ChangeRequiredStaff(cleaningRequiredStaffEntities);

            
            
            await _unitOfWork.CommitAsync();



            op.ChangePhases(preparation, surgery, cleaning);
            await this._unitOfWork.CommitAsync();

            return op;

        }

    }
}
