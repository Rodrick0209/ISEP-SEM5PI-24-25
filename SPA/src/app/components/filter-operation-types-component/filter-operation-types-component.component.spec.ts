import { ComponentFixture, TestBed } from '@angular/core/testing';

import { FilterOperationTypesComponentComponent } from './filter-operation-types-component.component';

describe('FilterOperationTypesComponentComponent', () => {
  let component: FilterOperationTypesComponentComponent;
  let fixture: ComponentFixture<FilterOperationTypesComponentComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [FilterOperationTypesComponentComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(FilterOperationTypesComponentComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
