using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationRequestLoggers;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationTypeLoggers;
using DDDSample1.Domain.OperationTypes;


namespace DDDSample1.Domain.OperationTypeLoggers
{

    public interface IOperationTypeLoggerRepository:IRepository<OperationTypeLogger,OperationTypeLoggerId>
    {
        Task<List<OperationTypeLogger>> GetAllAsync();

    }



}