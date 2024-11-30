
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationRooms;


namespace DDDSample1.Domain.OperationRooms
{

    public interface IOperationRoomService
    {

        Task<List<OperationRoom>> GetAllAsync();

        Task<List<OperationRoom>> GetOccupiedAsync(DateOnly date, TimeOnly time);
    }


}