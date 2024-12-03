using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using MastersData.Domain.RoomTypes;

namespace DDDSample1.Domain.RoomTypes
{
    public interface IRoomTypeService
    {
        Task<RoomTypeDto> AddRoomTypeAsync(AddRoomTypeDto dto);
        Task<RoomTypeDto> GetByIdAsync(RoomTypeId id);
        Task<List<RoomTypeDto>> GetAllAsync();
        Task RemoveRoomTypeAsync(string id);
    }
}