using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.OperationRooms
{
    public class OperationRoomRepository : BaseRepository<OperationRoom, OperationRoomId>, IOperationRoomRepository
    {
        private readonly DDDSample1DbContext context;

        public OperationRoomRepository(DDDSample1DbContext context) : base(context.OperationRooms)
        {
            this.context = context;
        }

        public async Task<List<OperationRoom>> GetAllAsync()
        {
            return await this.context.OperationRooms
                .ToListAsync(); // Retorna uma lista
        }

        public async Task<OperationRoom> GetByNameAsync(string name)
        {
            return await this.context.OperationRooms.Include(o => o.Appointments).FirstOrDefaultAsync(p => p.RoomNumber.roomNumber == name);
        }



    }
}
