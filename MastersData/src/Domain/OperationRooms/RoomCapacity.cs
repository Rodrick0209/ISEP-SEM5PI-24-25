using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationRooms
{
    public class RoomCapacity : IValueObject
    {
        public string roomCapacity { get; private set; }

        public RoomCapacity(string capacity)
        {
            int capacityValue;
            if (!int.TryParse(capacity, out capacityValue) || capacityValue <= 0)
            {
                throw new ArgumentException("Room capacity must be a positive integer.");
            }
            roomCapacity = capacity;
        }

        public void EnsureWithinCapacity(int numberOfPeople)
        {
            int capacityValue = int.Parse(roomCapacity);
            if (numberOfPeople > capacityValue)
            {
                throw new InvalidOperationException("The number of people exceeds the room capacity.");
            }
        }

        public override string ToString()
        {
            return roomCapacity.ToString();
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
                return false;

            RoomCapacity other = (RoomCapacity)obj;
            return roomCapacity == other.roomCapacity;
        }

        public override int GetHashCode()
        {
            return roomCapacity.GetHashCode();
        }
    }
}
