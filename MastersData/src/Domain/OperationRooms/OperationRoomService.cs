using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.OperationRooms
{
    public class OperationRoomService
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


    }
}
