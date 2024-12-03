#nullable enable
namespace MastersData.Domain.RoomTypes
{
    public class AddRoomTypeDto
    {
        public required string InternalCode { get; set; }
        public required string Designation { get; set; }
        public string? Description { get; set; }
        public required bool SuitableForSurgeries { get; set; }
    }
}