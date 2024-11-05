using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationRooms
{
    public class RoomType : IValueObject
    {
        public string roomType { get; private set; }

        private RoomType() { }

        public RoomType(string type)
        {
            if (string.IsNullOrWhiteSpace(type))
            {
                throw new ArgumentException("Room type cannot be empty or null.");
            }
            roomType = type;
        }

        public override string ToString()
        {
            return roomType;
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
                return false;

            RoomType other = (RoomType)obj;
            return roomType.Equals(other.roomType, StringComparison.OrdinalIgnoreCase);
        }

        public override int GetHashCode()
        {
            return roomType.GetHashCode(StringComparison.OrdinalIgnoreCase);
        }
    }
}
