using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationRequestLoggers;
using System.Collections.Generic;
using System.Threading.Tasks;


namespace DDDSample1.Domain.OperationTypeLoggers
{

    public interface IOperationTypeLoggerRepository:IRepository<OperationRequestLogger,OperationRequestLoggerId>
    {
    }


}