using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationTypes;




namespace DDDSample1.Domain.OperationTypes
{
    public interface IOperationTypeRepository:IRepository<OperationType, OperationTypeId>
    {
        Task<OperationType> GetByNameAsync(string name);
        Task<OperationType> GetByIdAsync(OperationTypeId id);

    }
}