using System;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;



namespace DDDSample1.Infrastructure.OperationTypes
{

    public class PhasesRepository : BaseRepository<Phase, PhasesId>, IPhasesRepository
    {
        private readonly DDDSample1DbContext context;


        public PhasesRepository(DDDSample1DbContext context):base(context.Phases)
        {
            this.context = context;
        }

        

        public async Task<Phase> GetByIdAsync(PhasesId id)
        {
            return await this.context.Phases.FirstOrDefaultAsync(u => u.Id.Value == id.Value);

        }



    }



}