using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.RoomTypes;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.RoomTypes
{
    public class RoomTypeRepository : BaseRepository<RoomType, RoomTypeId>, IRoomTypeRepository
    {
        private readonly DDDSample1DbContext _context;

        public RoomTypeRepository(DDDSample1DbContext context) : base(context.RoomTypes)
        {
            _context = context;
        }

        public async Task<RoomType> GetByInternalCodeAsync(string internalCode)
        {
            return await this._context.RoomTypes
                .Where(rt => rt.InternalCode.internalCode == internalCode)
                .FirstOrDefaultAsync();
        }
    }
}