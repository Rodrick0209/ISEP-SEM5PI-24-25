using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationTypeLoggers;
using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.OperationTypes;

namespace DDDSample1.Domain.OperationTypeLoggers
{


    public class OperationTypeLoggerService
    {

        private readonly IUnitOfWork unitOfWork;
        private readonly IOperationTypeLoggerRepository _repo;



        public OperationTypeLoggerService (IUnitOfWork unitOfWork, IOperationTypeLoggerRepository repo)
        {
            this.unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<List<OperationTypeLogger>> GetAllAsync()
        {    
            return await this._repo.GetAllAsync();
        }


         public async Task<OperationTypeLogger> CreateAsync(OperationTypeLogger obj)
        {
            
            await this._repo.AddAsync(obj);

            await this.unitOfWork.CommitAsync();

            return obj;

        }




    }









}