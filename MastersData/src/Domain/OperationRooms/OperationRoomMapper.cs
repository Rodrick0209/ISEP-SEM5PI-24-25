using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Shared;


namespace DDDSample1.Domain.OperationRooms
{
    public class OperationRoomMapper
    {
        public static OperationRoomDto ToDTO(OperationRoom operationRoom)
        {
            // Mapeando os slots de manutenção para DTOs
            List<MaintenanceSlotsDto> maintenanceSlots = operationRoom.MaintenanceSlots.ConvertAll(MaintenanceSlotMapper.ToDto);


            return new OperationRoomDto(
                operationRoom.Id.AsString(),
                operationRoom.RoomNumber.roomNumber,
                operationRoom.RoomType.roomType,
                operationRoom.RoomCapacity.roomCapacity,
                operationRoom.RoomStatus.ToString(),
                maintenanceSlots
            );
        }


    }
}
