import { Entity } from "../core/domain/Entity";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import IAllergyCathalogItemDTO from "../dto/IAllergyCatalogItemDTO";
import { AggregateRoot } from "../core/domain/AggregateRoot";



interface AllergyCatalogItemProps {
    name: string;
}


export class AllergyCatalogItem extends AggregateRoot<AllergyCatalogItemProps> {
    get id() : UniqueEntityID {
        return this._id;
    }

    get allergyId (): UniqueEntityID {
        return this.id;
    }


    get name (): string {
        return this.props.name;
    }

    private constructor (props: AllergyCatalogItemProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create (allergyCatalogDTO: IAllergyCathalogItemDTO, id?: UniqueEntityID): Result<AllergyCatalogItem> {
        const name = allergyCatalogDTO.name;

        if (!!name === false || name.length === 0) {
            return Result.fail<AllergyCatalogItem>('Must provide a name for the allergy')
        } else {
            const allergyCatalog = new AllergyCatalogItem({ name: name }, id);
            return Result.ok<AllergyCatalogItem>( allergyCatalog )
        }
    }

}