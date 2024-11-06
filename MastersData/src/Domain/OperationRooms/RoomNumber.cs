using System;
using DDDSample1.Domain.Shared;
using System.Text.RegularExpressions;

namespace DDDSample1.Domain.OperationRooms
{
    public class RoomNumber : IValueObject
    {
        private static int _nextNumber = 1;
        public string roomNumber { get; private set; }

        public RoomNumber()
        {
            roomNumber = GenerateNextNumber();
        }

        public RoomNumber(string number)
        {
            ValidateRoomNumber(number);
            roomNumber = number;
        }

        private static string GenerateNextNumber()
        {
            return $"or{_nextNumber++}";
        }

        private void ValidateRoomNumber(string number)
        {
            if (!Regex.IsMatch(number, @"^or\d+$"))
            {
                throw new ArgumentException("Room number must follow the format 'or' followed by an integer (e.g., 'or1', 'or2').");
            }
        }

        public override string ToString()
        {
            return roomNumber;
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
                return false;

            RoomNumber other = (RoomNumber)obj;
            return roomNumber == other.roomNumber;
        }

        public override int GetHashCode()
        {
            return roomNumber.GetHashCode();
        }
    }
}
