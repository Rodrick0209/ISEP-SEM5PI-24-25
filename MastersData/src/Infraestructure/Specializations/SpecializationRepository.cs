using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Specializations;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.Specializations
{

    public class SpecializationRepository : BaseRepository<Specialization, SpecializationId>, ISpecializationRepository
    {

        private readonly DDDSample1DbContext context;


        public SpecializationRepository(DDDSample1DbContext context):base(context.Specializations)
        {
            this.context = context;
        }

        public async Task<Specialization> GetByIdAsync(SpecializationId id)
        {
            return await this.context.Specializations.FirstOrDefaultAsync(p => p.Id == id);
        }

        
    }

}

