using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationType;



namespace DDDSample1.Infrastructure.OperationTypes
{

    internal class OperationTypeEntityTypeConfiguration : IEntityTypeConfiguration<OperationType>
    {

        public void Configure(EntityTypeBuilder<OperationType> builder)
        {
            
        }



    }





}
