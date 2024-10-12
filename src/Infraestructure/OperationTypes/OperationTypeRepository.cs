using DDDSample1.Domain.OperationType;
using DDDSample1.Infrastructure.Shared;



namespace DDDSample1.Infrastructure.OperationTypes
{

    public class OperationTypeRepository : BaseRepository<OperationType, OperationTypeId>, IOperationTypeRepository
    {


        public OperationTypeRepository(DDDSample1DbContext context):base(context.OperationTypes)
        {
            
        }





    }





}