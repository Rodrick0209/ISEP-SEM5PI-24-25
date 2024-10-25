using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationTypes;
using System.Collections.Generic;




namespace DDDSample1.Domain.OperationTypes
{
    public interface IOperationTypeRepository:IRepository<OperationType, OperationTypeId>
    {
        Task<OperationType> GetByNameAsync(string name);
        Task<OperationType> GetByIdAsync(OperationTypeId id);

        Task<List<OperationType>> GetAllAsync();

        Task<List<OperationType>> GetOperationTypesByFilter(string name, string status, string specialization);

    }
}