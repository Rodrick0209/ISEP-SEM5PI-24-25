using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure;
using Microsoft.AspNetCore.JsonPatch.Internal;
using System.Collections.Generic;

namespace DDDSample1.Domain.OperationTypes
{
    public class OperationTypeService : IOperationTypeService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IOperationTypeRepository _repo;

        public OperationTypeService(IUnitOfWork unitOfWork, IOperationTypeRepository operationTypeRepository)
        {
            _unitOfWork = unitOfWork;
            _repo = operationTypeRepository;
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

        
    }
}
