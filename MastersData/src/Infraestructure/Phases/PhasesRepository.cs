using System;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationType;
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


    }



}