using DDDSample1.Infrastructure.Shared;
using DDDSample1.Domain.OperationRequestLoggers;
using System.Threading.Tasks;
using System.Collections.Generic;
using Microsoft.EntityFrameworkCore;



namespace DDDSample1.Infrastructure.OperationRequestLoggers
{

    public class OperationRequestLoggerRepository : BaseRepository<OperationRequestLogger, OperationRequestLoggerId>, IOperationRequestLoggerRepository
    {

        private readonly DDDSample1DbContext context;

        public OperationRequestLoggerRepository(DDDSample1DbContext context):base(context.OperationRequestLoggers)
        {
            this.context = context;
        }


    }



}