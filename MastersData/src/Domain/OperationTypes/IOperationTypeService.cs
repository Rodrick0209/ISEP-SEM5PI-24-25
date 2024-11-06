using System.Threading.Tasks;
using DDDSample1.Domain.OperationTypes;
using System.Collections.Generic;
using System;

namespace DDDSample1.Domain.OperationTypes
{
    public interface IOperationTypeService
    {
        Task<OperationType> CreateAsync(OperationType obj);

        Task<OperationType> GetByIdAsync(OperationTypeId id);

        Task<OperationType> Deactivate(OperationTypeId id);

        Task<List<OperationType>> GetAllAsync();

        Task<List<OperationType>> GetOperationTypesByFilter(string name, string status, string specialization);

        Task<OperationType> UpdateAsync(OperationTypeDto dto,Dictionary<string, Guid> map);
    }
}