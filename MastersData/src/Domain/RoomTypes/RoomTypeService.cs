using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using MastersData.Domain.RoomTypes;

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

        public async Task<RoomTypeDto> AddRoomTypeAsync(AddRoomTypeDto dto)
        {
            await validateInternalCodeIsUnique(dto.InternalCode);

            var roomType = new RoomType(dto.InternalCode, dto.Designation, dto.Description, dto.SultabilityForSurgeries);

            await _roomTypeRepository.AddAsync(roomType);
            await _unitOfWork.CommitAsync();

            return new RoomTypeDto(roomType.Id.AsGuid(), roomType.InternalCode.internalCode, roomType.Designation.fullName, roomType.Description.description, roomType.SultabilityForSurgeries.sultabilityForSurgeries);
        }

        public async Task<List<RoomTypeDto>> GetAllAsync()
        {
            return (await _roomTypeRepository.GetAllAsync())
                .Select(roomType => new RoomTypeDto(roomType.Id.AsGuid(), roomType.InternalCode.internalCode, roomType.Designation.fullName, roomType.Description.description, roomType.SultabilityForSurgeries.sultabilityForSurgeries))
                .ToList();
        }

        public async Task<RoomTypeDto> GetByIdAsync(RoomTypeId id)
        {
            var roomType = await _roomTypeRepository.GetByIdAsync(id);
            return roomType == null ? null : new RoomTypeDto(roomType.Id.AsGuid(), roomType.InternalCode.internalCode, roomType.Designation.fullName, roomType.Description.description, roomType.SultabilityForSurgeries.sultabilityForSurgeries);
        }

        public async Task RemoveRoomTypeAsync(string id)
        {
            var roomType = await _roomTypeRepository.GetByIdAsync(new RoomTypeId(id));
            _roomTypeRepository.Remove(roomType);
            await _unitOfWork.CommitAsync();
        }

        private async Task validateInternalCodeIsUnique(string internalCode)
        {
            var roomType = await _roomTypeRepository.GetByInternalCodeAsync(internalCode);
            if (roomType != null)
            {
                throw new BusinessRuleValidationException("Room type internal code must be unique.");
            }
        }
    }
}