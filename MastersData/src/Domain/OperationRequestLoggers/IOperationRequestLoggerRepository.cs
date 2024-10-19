using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationRequestLoggers;
using System.Collections.Generic;
using System.Threading.Tasks;


namespace DDDSample1.Domain.OperationRequestLoggers
{

    public interface IOperationRequestLoggerRepository:IRepository<OperationRequestLogger,OperationRequestLoggerId>
    {
    }


}