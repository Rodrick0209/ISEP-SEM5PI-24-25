using DDDSample1.Infrastructure.Shared;
using DDDSample1.Domain.OperationTypeLoggers;
using System.Threading.Tasks;
using System.Collections.Generic;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.OperationTypes;



namespace DDDSample1.Infrastructure.OperationTypeLoggers
{

    public class OperationTypeLoggerRepository : BaseRepository<OperationTypeLogger, OperationTypeLoggerId>, IOperationTypeLoggerRepository
    {

        private readonly DDDSample1DbContext context;

        public OperationTypeLoggerRepository(DDDSample1DbContext context):base(context.OperationTypeLoggers)
        {
            this.context = context;
        }

        public async Task<List<OperationTypeLogger>> GetAllAsync()
        {
            return await this.context.OperationTypeLoggers
                .ToListAsync();
        }

    }



}