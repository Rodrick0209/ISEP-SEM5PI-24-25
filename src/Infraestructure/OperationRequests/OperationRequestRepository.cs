using DDDSample1.Infrastructure.Shared;
using DDDSample1.Domain.OperationRequest;



namespace DDDSample1.Infrastructure.OperationRequests
{

    public class OperationRequestRepository : BaseRepository<OperationRequest, OperationRequestId>, IOperationRequestRepository
    {

        public OperationRequestRepository(DDDSample1DbContext context):base(context.OperationRequests)
        {
            
        }



    }

}