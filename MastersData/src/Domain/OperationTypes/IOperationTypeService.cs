using System.Threading.Tasks;
using DDDSample1.Domain.OperationTypes;
using System.Collections.Generic;

namespace DDDSample1.Domain.OperationTypes
{
    public interface IOperationTypeService
    {
        Task<OperationType> CreateAsync(OperationType obj);

        Task<OperationType> GetByIdAsync(OperationTypeId id);

        Task<OperationType> Deactivate(OperationTypeId id);

        Task<List<OperationType>> GetAllAsync();

        Task<OperationType> UpdateAsync(OperationTypeDto dto);
    }
}