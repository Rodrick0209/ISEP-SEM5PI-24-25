using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;



namespace DDDSample1.Infrastructure.OperationTypes
{

    public class OperationTypeRepository : BaseRepository<OperationType, OperationTypeId>, IOperationTypeRepository
    {
        private readonly DDDSample1DbContext context;


        public OperationTypeRepository(DDDSample1DbContext context):base(context.OperationTypes)
        {
            this.context = context;
        }

        public async Task<OperationType> GetByNameAsync(string name)
        {
            return await this.context.OperationTypes.FirstOrDefaultAsync(u => u.name == name);

        }

        public async Task<OperationType> GetByIdAsync(OperationTypeId id)
        {
            return await this.context.OperationTypes
                .Include(o => o.preparationPhase)
                .Include(o => o.surgeryPhase)
                .Include(o => o.cleaningPhase)
                .FirstOrDefaultAsync(o => o.Id == id);
        }

        public async Task<List<OperationType>> GetAllAsync()
        {
            return await this.context.OperationTypes
                .Include(o => o.preparationPhase)
                .Include(o => o.surgeryPhase)
                .Include(o => o.cleaningPhase)
                .ToListAsync();
        }
        
    }



}