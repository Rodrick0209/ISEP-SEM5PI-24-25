using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure.Families;



namespace DDDSample1.Domain.OperationRequest
{
    public class OperationRequestService
    {
    private readonly IUnitOfWork _unitOfWork;

    private readonly IOperationRequestRepository _repo;

    public OperationRequestService(IUnitOfWork unitOfWork, IOperationRequestRepository repo)
    {
        this._unitOfWork = unitOfWork;
        this._repo = repo;
    }


    public async Task<OperationRequest> AddAsync(OperationRequest operationRequest)
    {
        await this._repo.AddAsync(operationRequest);

        await this._unitOfWork.CommitAsync();

        return operationRequest;

    }
    

    public async Task<OperationRequest> GetByIdAsync(OperationRequestId id)
    {
        var op = await this._repo.GetByIdAsync(id);
        if (op == null)
            return null;

        return op;

    }

}
}