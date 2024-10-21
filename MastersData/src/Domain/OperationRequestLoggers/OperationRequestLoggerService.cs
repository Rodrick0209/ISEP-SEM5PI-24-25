using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationRequestLoggers;
using System.Threading.Tasks;
using System.Collections.Generic;

namespace DDDSample1.Domain.OperationRequestLoggers
{


    public class OperationRequestLoggerService
    {

        private readonly IUnitOfWork unitOfWork;
        private readonly IOperationRequestLoggerRepository _repo;



        public OperationRequestLoggerService (IUnitOfWork unitOfWork, IOperationRequestLoggerRepository repo)
        {
            this.unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<List<OperationRequestLogger>> GetAllAsync()
        {    
            return await this._repo.GetAllAsync();
        }




    }









}