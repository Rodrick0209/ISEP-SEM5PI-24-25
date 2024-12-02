using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDSample1.Domain.RoomTypes
{
    public interface IRoomTypeService
    {
        Task<RoomTypeDto> AddRoomTypeAsync(string name);
        Task<RoomTypeDto> GetByIdAsync(string id);
        Task<List<RoomTypeDto>> GetAllAsync();
        Task RemoveRoomTypeAsync(string id);
    }
}