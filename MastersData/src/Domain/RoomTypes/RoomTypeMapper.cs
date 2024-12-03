namespace DDDSample1.Domain.RoomTypes
{
    public class RoomTypeMapper
    {
        public static RoomTypeDto toDTO(RoomType roomType)
        {
            return new RoomTypeDto(roomType.Id.AsGuid(), roomType.InternalCode.internalCode, roomType.Designation.fullName, roomType.Description?.description ?? null, roomType.SuitableForSurgeries.suitableForSurgeries);
        }

        public static RoomType toDomain(RoomTypeDto roomTypeDto)
        {
            return new RoomType(roomTypeDto.InternalCode, roomTypeDto.Designation, roomTypeDto.Description, roomTypeDto.SuitableForSurgeries);
        }
    }
}