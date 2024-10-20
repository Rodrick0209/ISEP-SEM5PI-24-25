using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationTypes;



namespace DDDSample1.Infrastructure.OperationTypes
{

    internal class OperationTypeEntityTypeConfiguration : IEntityTypeConfiguration<OperationType>
    {

        public void Configure(EntityTypeBuilder<OperationType> builder)
        {
                builder.HasKey(b => b.Id);
                
        }



    }





}
