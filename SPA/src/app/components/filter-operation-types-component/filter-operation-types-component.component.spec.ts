import { ComponentFixture, TestBed } from '@angular/core/testing';

import { FilterOperationTypesComponent } from './filter-operation-types-component.component';

describe('FilterOperationTypesComponentComponent', () => {
  let component: FilterOperationTypesComponent;
  let fixture: ComponentFixture<FilterOperationTypesComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [FilterOperationTypesComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(FilterOperationTypesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
