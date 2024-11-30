using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationRooms
{
    public class OperationRoomService : IOperationRoomService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IOperationRoomRepository _operationRoomRepository;

        public OperationRoomService(IUnitOfWork unitOfWork, IOperationRoomRepository operationRoomRepository)
        {
            this._unitOfWork = unitOfWork;
            this._operationRoomRepository = operationRoomRepository;
        }

        public async Task<List<OperationRoom>> GetAllAsync()
        {
            return await this._operationRoomRepository.GetAllAsync();
        }


        public async Task<OperationRoomDto> GetByIdAsync(OperationRoomId id)
        {

            var app = await _operationRoomRepository.GetByIdAsync(id);

            return app == null ? null : OperationRoomMapper.ToDTO(app);

        }



    }
}
