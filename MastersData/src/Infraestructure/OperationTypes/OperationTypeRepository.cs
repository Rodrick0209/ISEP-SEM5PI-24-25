using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;



namespace DDDSample1.Infrastructure.OperationTypes
{

    public class OperationTypeRepository : BaseRepository<OperationType, OperationTypeId>, IOperationTypeRepository
    {
        private readonly DDDSample1DbContext context;


        public OperationTypeRepository(DDDSample1DbContext context) : base(context.OperationTypes)
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

        public async Task<List<OperationType>> GetOperationTypesByFilter(string name, string status, string specialization)
        {
            var query = this.context.OperationTypes.AsQueryable();

            if (!string.IsNullOrEmpty(name))
            {
                query = query.Where(o => o.name == name);
            }

            if (!string.IsNullOrEmpty(specialization))
            {
                query = query.Where(o => o.specialization.Value == specialization);
            }

            var result = await query
                .Include(o => o.preparationPhase)
                .Include(o => o.surgeryPhase)
                .Include(o => o.cleaningPhase)
                .ToListAsync();

            // Perform client-side filtering for status
            if (!string.IsNullOrEmpty(status))
            {
                result = result.Where(o => o.GetStatusAsString().ToLower() == status.ToLower()).ToList();
            }

            return result;
        }
    }



}