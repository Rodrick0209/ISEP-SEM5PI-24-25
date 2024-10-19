using System.Threading.Tasks;
using DDDSample1.Domain.OperationType;

namespace DDDSample1.Domain.OperationType
{
    public interface IOperationTypeService
    {
        Task<OperationType> CreateAsync(OperationType obj);

        Task<OperationType> GetByIdAsync(OperationTypeId id);

        Task<OperationType> Deactivate(OperationTypeId id);
    }
}