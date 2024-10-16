using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;


namespace DDDSample1.Domain.OperationRequest
{

    public interface IOperationRequestRepository:IRepository<OperationRequest,OperationRequestId>
    {
            Task<List<OperationRequest>> GetOperationRequestsWithFilters(OperationRequestFilterDto filters, string doctorId);

    }


}