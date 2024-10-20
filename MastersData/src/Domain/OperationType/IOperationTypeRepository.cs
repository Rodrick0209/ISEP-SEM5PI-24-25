using System.Threading.Tasks;
using DDDSample1.Domain.Shared;




namespace DDDSample1.Domain.OperationType
{
    public interface IOperationTypeRepository:IRepository<OperationType, OperationTypeId>
    {
        Task<OperationType> GetByNameAsync(string name);
        Task<OperationType> GetByIdAsync(OperationTypeId id);

    }
}