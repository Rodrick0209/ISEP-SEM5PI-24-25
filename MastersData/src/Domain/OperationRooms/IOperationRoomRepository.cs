using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationRooms
{
    public interface IOperationRoomRepository : IRepository<OperationRoom, OperationRoomId>
    {
        Task<List<OperationRoom>> GetAllAsync();
        Task<OperationRoom> GetByNameAsync(string name);
        
        
    }
}
