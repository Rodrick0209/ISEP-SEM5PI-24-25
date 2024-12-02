using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.RoomTypes
{
    public class RoomTypeService : IRoomTypeService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IRoomTypeRepository _roomTypeRepository;

        public RoomTypeService(IUnitOfWork unitOfWork, IRoomTypeRepository roomTypeRepository)
        {
            this._unitOfWork = unitOfWork;
            this._roomTypeRepository = roomTypeRepository;
        }

        public async Task<RoomTypeDto> AddRoomTypeAsync(string name)
        {
            await validateNameIsUniqueAsync(name);

            var roomType = new RoomType(name);

            await _roomTypeRepository.AddAsync(roomType);
            await _unitOfWork.CommitAsync();

            return new RoomTypeDto(roomType.Id.AsGuid(), roomType.Name.fullName);
        }

        public async Task<List<RoomTypeDto>> GetAllAsync()
        {
            return (await _roomTypeRepository.GetAllAsync())
                .Select(roomType => new RoomTypeDto(roomType.Id.AsGuid(), roomType.Name.fullName))
                .ToList();
        }

        public async Task<RoomTypeDto> GetByIdAsync(string id)
        {
            var roomType = await _roomTypeRepository.GetByIdAsync(new RoomTypeId(id));
            return roomType == null ? null : new RoomTypeDto(roomType.Id.AsGuid(), roomType.Name.fullName);
        }

        public async Task RemoveRoomTypeAsync(string id)
        {
            var roomType = await _roomTypeRepository.GetByIdAsync(new RoomTypeId(id));
            _roomTypeRepository.Remove(roomType);
            await _unitOfWork.CommitAsync();
        }

        private async Task validateNameIsUniqueAsync(string name)
        {
            var roomType = await _roomTypeRepository.GetByNameAsync(name);
            if (roomType != null)
            {
                throw new BusinessRuleValidationException("Room type name must be unique.");
            }
        }
    }
}